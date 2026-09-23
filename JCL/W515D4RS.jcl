//W515D4RS JOB (650W5100100W515D4RS,W100),'RTN W515D4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W515D4                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W515.W515D3.W51574',                                           
//           T1='W515.W515D4.W51574',RF1=VB,LR1=755,CP1=15,                     
//*                                                                             
//           F2='W515.W515D3.W5157O',                                           
//           T2='W515.W515D4.W5157O',RF2=FB,LR2=121,CP2=15,                     
//*                                                                             
//           F3='W515.W515D3.W5157P',                                           
//           T3='W515.W515D4.W5157P',RF3=FB,LR3=121,CP3=15                      
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W515D4RS                                         
