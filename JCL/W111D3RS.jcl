//W111D3RS JOB (650W1110100W111D3RS,W100),'RTN W111D3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM ,FORMS=1800,LINECT=0                                                  
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W111D3                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W111.W111D1.W11173',                                           
//           T1='W111.W111D3.W11173',RF1=FB,LR1=20                              
//SOP     EXEC WSOPEND,PROCESS=W111D3RS                                         
