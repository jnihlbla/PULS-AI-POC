//W114J076 JOB (640W1140100W114J076,W100),'RTN W114SA',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST1                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W114    EXEC W114P076                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W114J076                                         
