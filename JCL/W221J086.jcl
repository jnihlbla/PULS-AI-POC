//W221J086 JOB (640W2210100W221J086,W100),'RTN W221D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W221    EXEC W221P086                                                         
//W22186.W22186D1 DD *                                                          
DAG                                                                             
//*                                                                             
//TST    EXEC WEMPTST,DSIN=W221.W221D4.W22186(+1)                               
//*                                                                             
//ORD    EXEC WSOP,COND=(0,LT,TST.T),COMMAND='ORDER W221D5'                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221J086                                         
