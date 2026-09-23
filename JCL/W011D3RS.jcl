//W011D3RS JOB (650W0110100W011D3RS,W100),'RTN W011D3',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM ,FORMS=1800,LINECT=0                                                  
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W011D3                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W011.DUMMY.W01114',                                            
//           T1='W011.W011D3.W01114',RF1=VB,LR1=80,                             
//*                                                                             
//           F2='W092.W092D2.W09279',                                           
//           T2='W092.W011D3.W09279',RF2=VB,LR2=80,                             
//*                                                                             
//           F3='W092.W092X3SE.TIURPROD',                                       
//           T3='W092.W011D3.TIURPROD',RF3=FB,LR3=47                            
//SOP     EXEC WSOPEND,PROCESS=W011D3RS                                         
