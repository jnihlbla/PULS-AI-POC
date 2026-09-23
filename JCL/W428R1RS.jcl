//W428R1RS JOB (640W4280100W428R1RS,W100),'RTN W428R1',                         
//* ÄR UPPDRAGSKODEN OVAN RÄTT?????                                             
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W428R1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W479.W479V2.W4795F',                                           
//           T1='W479.W428R1.W4795F',RF1=FB,LR1=264,                            
//*                                                                             
//           F2='W428.W428V1.W42811',                                           
//           T2='W428.W428R1.W42811',RF2=FB,LR2=64,                             
//*                                                                             
//           F3='W428.W428V1.W42823',                                           
//           T3='W428.W428R1.W42823',RF3=FB,LR3=62,                             
//*                                                                             
//           F4='W428.W428V1.W4285A',                                           
//           T4='W428.W428R1.W4285A',RF4=FB,LR4=115,                            
//*                                                                             
//           F5='W428.W428V1.W4285B',                                           
//           T5='W428.W428R1.W4285B',RF5=FB,LR5=37                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W428R1RS                                         
