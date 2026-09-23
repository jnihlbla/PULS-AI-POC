//W510D2RS JOB (650W5100100W510D2RS,W100),'RTN W510D2',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W510D2                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W371.W371S4.W37160',                                           
//           T1='W371.W510D2.W37160',RF1=FB,LR1=265,CP1=15,                     
//*                                                                             
//           F2='W418.W418D2.W41830',                                           
//           T2='W418.W510D2.W41830',RF2=FB,LR2=265,CP2=15,                     
//*                                                                             
//           F3='W418.W418D2.W41833',                                           
//           T3='W418.W510D2.W41833',RF3=FB,LR3=110,CP3=15,                     
//*                                                                             
//           F4='W418.W418S2.W418AO',                                           
//           T4='W418.W510D2.W418AO',RF4=FB,LR4=110,CP4=15,                     
//*                                                                             
//           F5='W418.W418S3.W418C2',                                           
//           T5='W418.W510D2.W418C2',RF5=FB,LR5=265,CP5=15,                     
//*                                                                             
//           F6='W426.W426V1.W42635',                                           
//           T6='W426.W510D2.W42635',RF6=FB,LR6=265,CP6=15,                     
//*                                                                             
//           F7='W476.W476D5.W4763C',                                           
//           T7='W476.W510D2.W4763C',RF7=FB,LR7=265,CP7=15,                     
//*                                                                             
//           F8='W476.W476D5.W47660',                                           
//           T8='W476.W510D2.W47660',RF8=FB,LR8=265,CP8=15,                     
//*                                                                             
//           F9='W476.W476D5.W4768D',                                           
//           T9='W476.W510D2.W4768D',RF9=VB,LR9=261,CP9=15                      
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W510D2RS                                         
