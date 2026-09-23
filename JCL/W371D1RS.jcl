//W371D1RS JOB (650W3710100W371D1RS,W100),'RTN W371D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W371D1                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W460.W460S3.W46091',                                           
//           T1='W460.W371D1.W46091',RF1=FB,LR1=062,                            
//*                                                                             
//           F2='W371.W371S5.W3717H',                                           
//           T2='W371.W371D1.W3717H',RF2=FB,LR2=062                             
//SOP     EXEC WSOPEND,PROCESS=W371D1RS                                         
