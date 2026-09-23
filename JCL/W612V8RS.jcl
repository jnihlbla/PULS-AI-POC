//W612V8RS JOB (670W6120100W612V8RS,W100),'RTN W612V8',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINECT=0,FORMS=1800                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WBLOCK EXEC WBLOCK,NAME=W612V8                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W612.W612D6.W61249',                                           
//           T1='W612.W612V8.W61249',RF1=FB,LR1=119,                            
//           F2='W612.W612D6.W6121F',                                           
//           T2='W612.W612V8.W6121F',RF2=FB,LR2=79                              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W612V8RS                                         
/*                                                                              
