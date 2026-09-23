//W111D2RS JOB (640W1110100W111D2RS,W100),'RTN W111D2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST1                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W111D2                                               
//*                                                                             
//*RENAME ONLY ONE GENERATION AT A TIME. WE CANNOT PROCESS MULTIPLE             
//*XML FILES.                                                                   
//*                                                                             
//RENAME   EXEC W001PTSO                                                        
//TSO.SYSTSIN DD *                                                              
 %WRTNIN2  W111.W111X7SE.W11150 W111.W111D2.W11150                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W111D2RS                                         
