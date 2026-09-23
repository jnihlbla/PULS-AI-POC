//WF10J0X1 JOB (640WF100100WF10J0X1,W100),'RTN WF10X1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=WF10XX                                         
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=WF10.&VCOM..WF1011(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(41,(100,10),RLSE),AVGREC=K,                               
//             DCB=(RECFM=FB,LRECL=41),                                         
//             MGMTCLAS=NOBACKUP                                                
//*                                                                             
//SOP     EXEC WSOP,COMMAND='ORDER WF10R1'                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=WF10J0X1                                         
