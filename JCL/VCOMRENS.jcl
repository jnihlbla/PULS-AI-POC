//WCOMRENS JOB (640WOS39000),'RENSA I VCOM',                                    
//             CLASS=K,MSGCLASS=H                                               
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=STD,LINECT=0                                                    
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=VCOMRENS                                       
//W01612.W01612D1 DD *                                                          
WF02X1SE                 --BARA EXEMPEL HELGE                                   
//*                                                                             
//W01612.W016XXD1 DD DSN=WIN.VCOMRENS(+1),                                      
//             DISP=(NEW,CATLG,DELETE),                                         
//             DCB=(RECFM=VB,LRECL=5000),                                       
//             MGMTCLAS=BACKUPC,DASTACLAS=PSEN                                  
