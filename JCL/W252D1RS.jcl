//W252D1RS JOB (640W2520100W252D1RS,W100),'RTN W252D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W252D1                                               
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//           F1='W236.W236D2.W23683',                                           
//           T1='W236.W252D1.W23683',RF1=FB,LR1=0021,                           
//*                                                                             
//           F2='W612.W612D4.W6125B',                                           
//           T2='W612.W252D1.W6125B',RF2=FB,LR2=0021                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W252D1RS                                         
