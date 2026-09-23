//W233PVRS JOB (650W2330100W233PVRS,W100),'RTN W233PV',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W233PV                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W233.W100V2.W23311',                                           
//           T1='W233.W233PV.W23311',RF1=FB,LR1=093,                            
//*                                                                             
//           F2='W612.W612V1.W61216',                                           
//           T2='W612.W233PV.W61216',RF2=FB,LR2=049                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W233PVRS                                         
