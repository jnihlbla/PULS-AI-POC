//W100V1RS JOB (650W0010300W100V1RS,W100),'RTN W100V1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W100V1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W426.W426V4.W42676',                                           
//           T1='W426.W100V1.W42676',RF1=FB,LR1=011,                            
//*                                                                             
//           F2='W215.W215S1.W21507',                                           
//           T2='W215.W100V1.W21507',RF2=VB,LR2=062                             
//SOP     EXEC WSOPEND,PROCESS=W100V1RS                                         
