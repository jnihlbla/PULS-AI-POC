//W479M3RS JOB (640W4790100W479M3RS,W100),'RTN W479M3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W479M3                                               
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//           F1=W479.W479V2.W47955B,                                            
//           T1=W479.W479M3.W47955B,RF1=FB,LR1=177                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W479M3RS                                         
