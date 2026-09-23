//W221J286 JOB (640W2210100W221J286,W100),'RTN W221V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W221    EXEC W221P286                                                         
//W22186.W22186D1 DD *                                                          
VEC                                                                             
//*                                                                             
//TST    EXEC WEMPTST,DSIN=W221.W221V2.W22186(+1)                               
//*                                                                             
//ORD    EXEC WSOP,COND=(0,LT,TST.T),COMMAND='ORDER W221D5'                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221J286                                         
