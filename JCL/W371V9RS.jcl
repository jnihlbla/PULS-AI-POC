//W371V9RS JOB (650W3710100W371V9RS,W100),'RTN W371V9',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W371V9                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W371.W371V7.W37178',                                           
//           T1='W371.W371V9.W37178',RF1=FB,LR1=117                             
//SOP     EXEC WSOPEND,PROCESS=W371V9RS                                         
