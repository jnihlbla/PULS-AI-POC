//W611D3RS JOB (640W6110100W611D3RS,W100),'RTN W611D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W611D3                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W092.W092D2.W09210',                                           
//           T1='W092.W611D3.W09210',RF1=VB,LR1=083,                            
//*                                                                             
//           F2='W476.W476D5.W47693',                                           
//           T2='W476.W611D3.W47693',RF2=VB,LR2=098                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611D3RS                                         
