//W233V1RS JOB (650W2330100W233V1RS,W100),'RTN W233V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//RENAME  EXEC WRTNINP,                                                         
//             F1='W233.W233X1SE.W23327',                                       
//             T1='W233.W233V1.W23327',RF1=FB,LR1=32                            
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W233V1RS                                         
