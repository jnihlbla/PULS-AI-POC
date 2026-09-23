//W488J0X1 JOB (540W4880100W488J0X1,W100),'RTN W488X1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W488X1                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
W488X1SE                                                                        
//*                                                                             
//W01612.W016XXD1 DD DSN=WIN.PDP.W48819(+1),                                    
//             DISP=(NEW,CATLG,DELETE),                                         
//             DATACLAS=PSEN,                                                   
//             DCB=(RECFM=VB,LRECL=0084),                                       
//             MGMTCLAS=BACKUPC                                                 
//*                                                                             
//SOP     EXEC WSOP,COMMAND='ORDER W488S1'                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W488J0X1                                         
