//W611J0X5 JOB (640W6110100W611J0X5,W100),'RTN W611X5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W611X5                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
W611X5SE                                                                        
//*                                                                             
//W01612.W016XXD1 DD DSN=W611.W611X5SE.W61160(+1),                              
//             DISP=(NEW,CATLG,DELETE),                                         
//             DATACLAS=PSEN,                                                   
//             DCB=(RECFM=VB,LRECL=0084),                                       
//             MGMTCLAS=NOBACKUP                                                
//*                                                                             
//SOP     EXEC WSOP,COMMAND='ORDER W611S5'                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J0X5                                         
