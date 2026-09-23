//W418D5RS JOB (640W4180100W418D5RS,W100),'RTN W418D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W418D5                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W414.W414D1.W41411',                                           
//           T1='W414.W418D5.W41411',RF1=FB,LR1=055,                            
//*                                                                             
//           F2='W612.W612DA.W61271',                                           
//           T2='W612.W418D5.W61271',RF2=FB,LR2=055,                            
//*                                                                             
//           F3='W612.W612D2.W61228',                                           
//           T3='W612.W418D5.W61228',RF3=FB,LR3=055                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W418D5RS                                         
