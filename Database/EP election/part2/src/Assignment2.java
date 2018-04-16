import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class Assignment2 extends JDBCSubmission {
	public Assignment2() throws ClassNotFoundException {
        Class.forName("org.postgresql.Driver");
    }

    @Override
    public boolean connectDB(String url, String username, String password) {
    	try{
    		connection = DriverManager.getConnection(url,username,password);
    		setSearchPath("parlgov");
    		return true;
    	}catch (SQLException e){return false;}
    }
    
    private void setSearchPath(String search_path) throws SQLException{
    	//another expression to set schema
    	//connection.setSchema(search_path);
    	Statement stmt = connection.createStatement();
        stmt.execute("SET search_path to "+search_path);
        stmt.close();
    }

    @Override
    public boolean disconnectDB() {
	    try {
	    	if(!noConnection()){connection.close();}
            return true;
	    } catch (SQLException e) {return false;}	
    }

    private boolean noConnection(){
        return (connection==null);
    }
    
    @Override
    public ElectionCabinetResult electionSequence(String countryName) {
    	if(noConnection()){return null;}
    	
    	String queryString;		
    	try{
    		//find election id as well as election date in given country
    		queryString = "SELECT "
    				+ "date_part('year',e.e_date) AS \"cur_date\", "
    				+ "e.id AS \"cur\" "
    				+ "FROM election e, country c "
    				+ "WHERE "
    				+ "c.name = ? AND "
    				+ "e.country_id = c.id "
    				+ "ORDER BY e.e_date DESC";
    		
    		PreparedStatement ps = connection.prepareStatement(queryString);
    		ps.setString(1,countryName);
        	ResultSet rs = ps.executeQuery();
    		
        	ElectionCabinetResult result = new ElectionCabinetResult( new ArrayList<Integer>(), new ArrayList<Integer>());
        	
        	Integer prvId = null;
        	Integer prv_date = null;
        	
        	while(rs.next()){
        		Integer curId = rs.getInt("cur");
        		Integer cur_date = rs.getInt("cur_date");
        		
        		if((prvId==null)&&(prv_date==null)){
        			result = addQualifyCabinet(result,curId,countryName,null,cur_date);
        		}else{
        			result = addQualifyCabinet(result,curId,countryName,prv_date,cur_date);
        		}
        		
        		prvId = curId;
    			prv_date = cur_date;
        	}
        	
    		return result;
    	}catch(SQLException e){return null;}
    }
    
    private ElectionCabinetResult addQualifyCabinet(ElectionCabinetResult result, Integer electionId,
    		String countryName, Integer large_date, Integer small_date) throws SQLException{
    	
    	//find out cabinet id in given time period
    	String queryString = "SELECT cab.start_date, cab.id "
    			+ "FROM cabinet cab, country c "
    			+ "WHERE "
    			+ "cab.country_id = c.id AND "
    			+ "c.name = ? AND "
    			+ "date_part('year',cab.start_date) >= ? ";
    	String preDateInfo = "AND date_part('year',cab.start_date) < ? ";
    	String order = "ORDER BY cab.start_date";
    	
    	//case that given time period beyond latest election year
    	if(large_date == null){queryString += order;}
    	else{queryString += preDateInfo + order;}
    	
    	PreparedStatement ps = connection.prepareStatement(queryString);
    	
    	ps.setString(1, countryName);
    	ps.setInt(2, small_date);
    	if(large_date != null){ps.setInt(3, large_date);};
    	
    	ResultSet rs = ps.executeQuery();
    	
    	while(rs.next()){
    		result.elections.add(electionId);
    		
    		int cabinetId = rs.getInt("id");
    		result.cabinets.add(cabinetId);
    	}
    	
    	return result;
    }

    @Override
    public List<Integer> findSimilarPoliticians(Integer politicianName, Float threshold) {
    	if(noConnection()){return null;}
    	
    	String queryString;		
    	try{
    		queryString = "SELECT "
    				+ "t1.id AS \"p1\",t2.id AS \"p2\", "
    				+ "t1.comment || t1.description AS \"p1_words\", "
    				+ "t2.comment || t2.description AS \"p2_words\" "
    				+ "FROM " 
    				+ "(SELECT id, comment, description "
    				+ "FROM politician_president "
    				+ "WHERE id = ?) AS t1, politician_president t2 "
    				+ "WHERE t1.id!=t2.id";
    		
    		PreparedStatement ps = connection.prepareStatement(queryString);
    		ps.setInt(1,politicianName);
        	ResultSet rs = ps.executeQuery();
        	
        	List<Integer> qualifiedPoliticianId = new ArrayList<Integer>();
        	
        	while(rs.next()){
        		int otherPoliticianId = rs.getInt("p2");
        		
        		String p1_words = rs.getString("p1_words");
        		String p2_words = rs.getString("p2_words");
        		if(similarity(p1_words,p2_words) > threshold){
        			qualifiedPoliticianId.add(otherPoliticianId);
        		}
        	}
    		
        	return qualifiedPoliticianId;
    	}catch(SQLException e){return null;}
    }

    public static void main(String[] args) throws ClassNotFoundException{
        // You can put testing code in here. It will not affect our autotester.
    	String url = "jdbc:postgresql://localhost:5432/";
	    String username = "postgres";
	    String password = "enhao0120";
    	
	    Assignment2 a2 = new Assignment2();
	    
	    System.out.println(a2.connectDB(url, username, password));
	    
	    System.out.println(a2.electionSequence("Canada"));
	    System.out.println(a2.findSimilarPoliticians(9, new Float(0.2)));
	    
	    System.out.println(a2.disconnectDB());
    }
}

