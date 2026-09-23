//W412J084 JOB (670W4120100W412J084,W100),'RTN W412S2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST4                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W412    EXEC W412P084                                                         
//W41284.W41284D0 DD *                                                          
&ODATE.&OTIME.                                                                  
//*                                                                             
//ACCMAIL EXEC WMAILSND,DSIN=W412.W412S2.W4128A(+1)                             
)SEND                                                                           
TITLE Orders received, file &OFILE                                              
TO &MAILID                                                                      
MAIL SEND                                                                       
)END                                                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W412J084                                         
