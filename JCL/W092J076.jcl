//W092J076 JOB (640W0920100W092J076,W100),'RTN W011D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST0                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W092    EXEC W092P076                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W092J076                                         
