//W479M2RS JOB (670W4790100W479M2RS,W100),'RTN W479M2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W479M2                                               
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//           F1='W479.W479V3.W4791A',                                           
//           T1='W479.W479M2.W4791A',RF1=F,LR1=16,                              
//*                                                                             
//           F2='W479.W479V2.W47955A',                                          
//           T2='W479.W479M2.W47955A',RF2=F,LR2=177                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W479M2RS                                         
