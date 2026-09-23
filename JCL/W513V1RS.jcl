//W513V1RS JOB (650W5130100W513V1RS,W100),'RTN W513V1',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W513V1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W513.W510D2.W5132D1',                                          
//           T1='W513.W513V1.W5132D1',RF1=VB,LR1=081,                           
//*                                                                             
//           F2='W510.W510D4.W51310',                                           
//           T2='W513.W513V1.W51310',RF2=FB,LR2=028,                            
//*                                                                             
//           F3='W570.W570D3.W51310',                                           
//           T3='W570.W513V1.W51310',RF3=FB,LR3=028,                            
//*                                                                             
//           F4='W515.W515D3.W51310',                                           
//           T4='W515.W513V1.W51310',RF4=FB,LR4=028,                            
//*                                                                             
//           F5='W561.W561D3.W51310',                                           
//           T5='W561.W513V1.W51310',RF5=FB,LR5=028,                            
//*                                                                             
//           F6='W570.W570D3.W51390',                                           
//           T6='W570.W513V1.W51390',RF6=FB,LR6=028,                            
//*                                                                             
//           F7='W570.W570D3.W51350',                                           
//           T7='W570.W513V1.W51350',RF7=FB,LR7=028,                            
//*                                                                             
//           F8='W570.W570D3.W51380',                                           
//           T8='W570.W513V1.W51380',RF8=FB,LR8=028,                            
//*                                                                             
//           F9='W570.W570D3.W51320',                                           
//           T9='W570.W513V1.W51320',RF9=FB,LR9=028                             
//*                                                                             
//RENAME02 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W570.W570D3.W51310A',                                          
//           T1='W570.W513V1.W51310A',RF1=FB,LR1=028,                           
//*                                                                             
//           F2='W570.W570D3.W51340',                                           
//           T2='W570.W513V1.W51340',RF2=FB,LR2=028,                            
//*                                                                             
//           F3='W570.W570D3.W51350B',                                          
//           T3='W570.W513V1.W51350B',RF3=FB,LR3=028,                           
//*                                                                             
//           F4='W570.W570D3.W51350M',                                          
//           T4='W570.W513V1.W51350M',RF4=FB,LR4=028,                           
//*                                                                             
//           F5='W570.W570D3.W51350Z',                                          
//           T5='W570.W513V1.W51350Z',RF5=FB,LR5=028                            
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W513V1RS                                         
