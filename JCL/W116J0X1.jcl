//W116J0X1 JOB (640W1160100W116J0X1,W100),'RTN W116X1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST1                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W016    EXEC W016P012,VCOMPARM=W116XX                                         
//*                                                                             
//W01612.W01612D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//W01612.W016XXD1 DD DSN=W116.&VCOM..W11601(+1),                                
//             DISP=(NEW,CATLG,DELETE),                                         
//             SPACE=(80,(1,1),RLSE),AVGREC=U,                                  
//             DCB=(RECFM=FB,LRECL=80),                                         
//             MGMTCLAS=NOBACKUP                                                
//*                                                                             
//W116    EXEC W116P080,                                                        
//             INDIN=W116.&VCOM                                                 
//*                                                                             
//W11680.W11680D1 DD *                                                          
&VCOM                                                                           
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W116J0X1                                         
