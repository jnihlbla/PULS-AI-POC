//W980V1RE JOB (650W0090100W980V1RE,W100),'RTN W980V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ    LOCAL                                                            
/*ROUTE PRINT  LOCAL                                                            
//*                                                                             
//NEWGEN  EXEC PGM=OPNCLOSE,PARM=DD                                             
//DD1      DD  DSN=W980.W980V1.W98032(+1),DISP=(NEW,CATLG,DELETE),              
//             RECFM=FB,LRECL=80,                                               
//             DATACLAS=PSEN,MGMTCLAS=BACKUPC                                   
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W980V1RE                                         
