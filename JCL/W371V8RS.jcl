//W371V8RS JOB (650W3710100W371V8RS,W100),'RTN W371V8',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W371V8                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W371.W371V5.W37108',                                           
//           T1='W371.W371V8.W37108',RF1=FB,LR1=159,                            
//*                                                                             
//           F2='W371.W371V6.W37136',                                           
//           T2='W371.W371V8.W37136',RF2=FB,LR2=159,                            
//*                                                                             
//           F3='W371.W371V7.W37131',                                           
//           T3='W371.W371V8.W37131',RF3=FB,LR3=159                             
//SOP     EXEC WSOPEND,PROCESS=W371V8RS                                         
