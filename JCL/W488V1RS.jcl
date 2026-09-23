//W488V1RS JOB (650W4880100W488V1RS,W100),'RTN W488V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W488V1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W488.PDP.W48830',                                              
//           T1='W488.W488V1.W48830',RF1=FB,LR1=38,                             
//*                                                                             
//           F2='W488.PDP.W48831',                                              
//           T2='W488.W488V1.W48831',RF2=FB,LR2=26                              
//SOP     EXEC WSOPEND,PROCESS=W488V1RS                                         
