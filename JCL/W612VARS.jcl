//W612VARS JOB (640W6120100W612VARS,W100),'RTN W612VA',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W612VA                                               
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W414.W414D1.W4140S',                                           
//           T1='W414.W612VA.W4140S',RF1=FB,LR1=59,                             
//*                                                                             
//           F2='W612.W612D6.W6124S',                                           
//           T2='W612.W612VA.W6124S',RF2=FB,LR2=59                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612VARS                                         
