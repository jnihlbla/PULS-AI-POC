//W463D5RS JOB (640W4630100W463D5RS,W100),'RTN W463D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W463D5                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W463.W463S9.W4635A',                                           
//           T1='WXTR.W463D5.W4635A',RF1=FB,LR1=200,CP1=10,                     
//*                                                                             
//           F2='W476.W476D5.W47658',                                           
//           T2='WXTR.W463D5.W47658',RF2=FB,LR2=200,CP2=10                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463D5RS                                         
