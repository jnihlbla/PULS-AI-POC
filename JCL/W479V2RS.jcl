//W479V2RS JOB (650W0010300W479V2RS,W100),'RTN W479V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//BLOCK   EXEC WBLOCK,NAME=W479V2                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W092.W092D6.W092Z4',                                           
//           T1='W092.W479V2.W092Z4',RF1=FB,LR1=022,                            
//*                                                                             
//           F2='W092.W092D6.W092Z3',                                           
//           T2='W092.W479V2.W092Z3',RF2=FB,LR2=022,                            
//*                                                                             
//           F3='W479.W479V2.W47967',                                           
//           T3='W479.TEMP.W47967',RF3=FB,LR3=059                               
//SOP     EXEC WSOPEND,PROCESS=W479V2RS                                         
