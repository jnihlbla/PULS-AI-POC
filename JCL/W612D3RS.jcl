//W612D3RS JOB (670W6120100W612D3RS,W100),'RTN W612D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINECT=0,FORMS=1800                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//WBLOCK EXEC WBLOCK,NAME=W612D3                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W612.W612D2.W61223',                                           
//           T1='W612.W612D3.W61223',RF1=FB,LR1=43                              
//*                                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W612D3RS                                         
/*                                                                              
