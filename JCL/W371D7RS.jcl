//W371D7RS JOB (640W3710100W371D7RS,W100),'RTN W371D7',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W371D7                                               
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//           F1='W479.WXTRD1.W47923B',                                          
//           T1='W479.W371D7.W47923B',RF1=FB,LR1=320,                           
//*                                                                             
//           F2='W479.WXTRD1.W47925X',                                          
//           T2='W479.W371D7.W47925X',RF2=FB,LR2=94,                            
//*                                                                             
//           F3='W479.W479D1.W47993',                                           
//           T3='W479.W371D7.W47993',RF3=FB,LR3=146                             
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371D7RS                                         
