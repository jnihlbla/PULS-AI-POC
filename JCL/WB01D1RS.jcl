//WB01D1RS JOB (640WB010100WB01D1RS,W100),'RTN WB01D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYSTB                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=WB01D1                                               
//*                                                                             
//RENAME   EXEC W001PTSO,                                                       
//          MEMBER=TEMPNAME,MAXRC=4                                             
//* RENAMA DEN ÄLDSTA GENERATIONEN, IFALL DET SKULLE FINNAS FLER                
//* DET ÄR VIKTIGT ATT DE KOMMER I RÄTT ORDNING                                 
//*                                                                             
//TSO.SYSTSIN DD *                                                              
 %WRTNIN2  WB01.WB01X1PP.WB0120   WB01.WB01D1.WB0120                            
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WB01D1RS                                         
