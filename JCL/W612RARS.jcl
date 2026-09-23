//W612RARS JOB (640W6120100W612RARS,W100),'RTN W612RA',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W612RA                                               
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W612.W612VA.W4140SV',                                          
//           T1='W612.W612RA.W4140SV',RF1=FB,LR1=59,                            
//*                                                                             
//           F2='W612.W612VA.W6124SV',                                          
//           T2='W612.W612RA.W6124SV',RF2=FB,LR2=59,                            
//*                                                                             
//           F3='W611.W611V1.W6119P',                                           
//           T3='W611.W612RA.W6119P',RF3=FB,LR3=59                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612RARS                                         
