//W611D1RS JOB (640W6110100W611D3RS,W100),'RTN W611D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W611D1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W611.W611S5.W61162',                                           
//           T1='W611.W611D1.W61162',RF1=FB,LR1=080,                            
//*                                                                             
//           F2='W611.W611S5.W61163',                                           
//           T2='WXTR.W611D1.W61163',RF2=FB,LR2=080                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611D1RS                                         
