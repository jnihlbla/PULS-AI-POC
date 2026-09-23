//W570M2RS JOB (650W5700100W570M2RS,W100),'RTN W570M2',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK  EXEC WBLOCK,NAME=W570M2                                                
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W570.W570D3.W57069',                                           
//           T1='W570.W570M2.W57069',RF1=FB,LR1=061,                            
//*                                                                             
//           F2='W570.W570D2.W57066A',                                          
//           T2='W570.W570M2.W57066A',RF2=FB,LR2=292                            
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W570M2RS                                         
