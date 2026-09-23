//W218D2RS JOB (650W2180100W218D2RS,W100),'RTN W218D2',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W218D2                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W111.W111D1.W11122',                                           
//           T1='W111.W218D2.W11122',RF1=FB,LR1=5,                              
//*                                                                             
//           F2='W111.W111D1.W11114',                                           
//           T2='W111.W218D2.W11114',RF2=FB,LR2=5,                              
//*                                                                             
//           F3='W222.W222D1.W22216',                                           
//           T3='W222.W218D2.W22216',RF3=FB,LR3=5                               
//SOP     EXEC WSOPEND,PROCESS=W218D2RS                                         
