//W010V9RS JOB (670W0010300W010V9RS,W100),'RTN W010V9',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W010V9                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W418.W418M1.W41882',                                           
//           T1='W418.W010V9.W41882',RF1=FB,LR1=18,                             
//*                                                                             
//           F2='W479.W479V3.W479A5',                                           
//           T2='W479.W010V9.W479A5',RF2=FB,LR2=24,                             
//*                                                                             
//           F3='W479.W479V3.W479E4',                                           
//           T3='W479.W010V9.W479E4',RF3=FB,LR3=27,                             
//*                                                                             
//           F4='W479.W479V3.W479E6',                                           
//           T4='W479.W010V9.W479E6',RF4=FB,LR4=08                              
//*                                                                             
//SPOC      EXEC PGM=CSLUSPOC,                                                  
//  PARM=('IMSPLEX=IMG0,ROUTE=*,WAIT=30')                                       
//SYSPRINT  DD SYSOUT=*                                                         
//SYSIN     DD *                                                                
 UPDATE DB NAME(W*) STOP(ACCESS)                                                
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W010V9RS                                         
