//W475M0RS JOB (650W4750100W475M0RS,W100),'RTN W475M0',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W475M0                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W418.W418D2.W41854',                                           
//           T1='W418.W475M0.W41854',RF1=FB,LR1=029,                            
//*                                                                             
//           F2='W510.W510D3.W51049',                                           
//           T2='W510.W475M0.W51049',RF2=FB,LR2=050,                            
//*                                                                             
//           F3='W522.W522M1.W52212',                                           
//           T3='W522.W475M0.W52212',RF3=FB,LR3=029                             
//SOP     EXEC WSOPEND,PROCESS=W475M0RS                                         
