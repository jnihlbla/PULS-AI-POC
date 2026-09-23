//W476J041 JOB (540W4760100W476J041,W100),'RTN W476SE',                         
//             CLASS=L,                                                         
//             USER=?,PASSWORD=?                                                
/*JOBPARM FORMS=1800,LINECT=0                                                   
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*ROUTE XEQ   LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W476    EXEC W476P041                                                         
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//* OM UTFIL (EDI TRANSPORTFIL) INTE ÄR TOM GÖRS EN KOPIA                       
//*                                                                             
//EMPTYT  EXEC WEMPTST,DSIN=W476.W476SE.W47641(+1)                              
//*                                                                             
//     IF (EMPTYT.T.RC = 0) THEN                                                
//*                                                                             
//GENER1   EXEC PGM=V16459,PARM='ICEGENER/3/'                                   
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W476.W476SE.W47641(+1),DISP=(OLD,PASS,DELETE)                
//************ FIL TILL EDI TRANSPORTFIL                                        
//*                                                                             
//SYSUT2   DD DSN=W476.W476SE.W47641C(+1),                    -LIM(255)         
//             DISP=(NEW,CATLG,DELETE),                                         
//************ KOPIAN AV EDI TRANSPORTFIL                                       
//             MGMTCLAS=&BACKUPC,DATACLAS=PSEN                                  
//*                                                                             
//     ENDIF                                                                    
//*                                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W476J041                                         
