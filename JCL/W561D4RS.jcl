//W561D4RS JOB (650W5100100W561D4RS,W100),'RTN W561D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W561D4                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W561.W561D3.W56174',                                           
//           T1='W561.W561D4.W56174',RF1=VB,LR1=755,CP1=15,                     
//*                                                                             
//           F2='W561.W561D3.W5617O',                                           
//           T2='W561.W561D4.W5617O',RF2=FB,LR2=121,CP2=15,                     
//*                                                                             
//           F3='W561.W561D3.W5617P',                                           
//           T3='W561.W561D4.W5617P',RF3=FB,LR3=121,CP3=15                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W561D4RS                                         
