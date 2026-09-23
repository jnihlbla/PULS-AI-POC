//W412J083 JOB (640W4120100W412J083,W100),'RTN W412S2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST4                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//* DATUM=&ODATE,TID=&OTIME,IDUSER=&OIDUSER                                     
//W412    EXEC W412P083                                                         
//W41283.W41283D0 DD *                                                          
&ODATE.&OTIME.&OIDUSER.                                                         
//*                                                                             
//* test if error file is empty, else send errmail                              
//EMPTEST EXEC WEMPTST,DSIN=W412.W412S2.W4128E(+1)                              
//*                                                                             
// IF   (EMPTEST.T.RC > 0) THEN                                                 
//EMPTEST2 EXEC WEMPTST,DSIN=W412.W412S2.W41285(+1)                             
// IF   (EMPTEST2.T.RC > 0) THEN                                                
//ORDER   EXEC WSOP,COMMAND='ORDER W412J084'                                    
//*                                                                             
// ELSE                                                                         
//SOP     EXEC WSOP                                                             
  ORDER W271S1 SYMBOLS                                                          
    MAILID(&MAILID)                                                             
  END-ORDER                                                                     
// ENDIF                                                                        
// ELSE                                                                         
//*       -- ERROR MAIL DATA, SEND MAIL AND STOP PROCESSING                     
//ERRMAIL EXEC WMAILSND,DSIN=W412.W412S2.W4128E(+1)                             
)SEND                                                                           
TITLE Errors in order file &OFILE                                               
TO &MAILID                                                                      
MAIL SEND                                                                       
)END                                                                            
// ENDIF                                                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W412J083                                         
