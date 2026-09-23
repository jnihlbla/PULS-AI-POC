//W478V1RS JOB (650W0010300W478V1RS,W100),'RTN W478V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W478V1                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W478.W478D2.W47840',                                           
//           T1='W478.W478V1.W47840',RF1=FB,LR1=034,                            
//*                                                                             
//           F2='W478.W478D2.W47841',                                           
//           T2='W478.W478V1.W47841',RF2=FB,LR2=048,                            
//*                                                                             
//           F3='W478.W478D2.W47843',                                           
//           T3='W478.W478V1.W47843',RF3=FB,LR3=081,CP3=5                       
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W478V1RS                                         
