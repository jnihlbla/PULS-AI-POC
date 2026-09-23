//W371D5RS JOB (650W3710100W371D5RS,W100),'RTN W371D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W371D5                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W476.W476D5.W4768M',                                           
//           T1='W476.W371D5.W4768M',RF1=FB,LR1=025,CP1=10,                     
//*                                                                             
//           F2='W418.W418D2.W418AG',                                           
//           T2='W418.W371D5.W418AG',RF2=FB,LR2=025,CP2=10                      
//SOP     EXEC WSOPEND,PROCESS=W371D5RS                                         
