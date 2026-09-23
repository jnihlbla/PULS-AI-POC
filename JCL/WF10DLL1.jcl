//WF10DLL1 JOB (650WF100100WF10DLL1,W100),'RTN WF10D5',                         
//             CLASS=K,                                                         
//             USER=?,PASSWORD=?                                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*CHANGE THE TABLE FROM T01DLIN TO T01DLIN_ARC IN LOAD CARD                    
//SORT1    EXEC PGM=SORT                                                        
//SYSOUT     DD SYSOUT=*                                                        
//SYSPRINT   DD SYSOUT=*                                                        
//SORTIN     DD DSN=WF01.SYSPUNCH.S01DLIN(0),DISP=SHR                           
//SYSIN      DD *                                                               
  OPTION COPY                                                                   
  OUTREC FINDREP=(IN=C'T01DLIN',OUT=C'T01DLIN_ARC')                             
//SORTOUT    DD DSN=&&DLINARC,                                                  
//             DISP=(NEW,PASS,DELETE),                                          
//             DATACLAS=PSEN                                                    
//*                                                                             
//*LOAD DATA TO T01DLIN_ARC                                                     
//LOAD     EXEC WG02LOAD,DSIN=WF01.DISCARD.S01DLIN(0),                          
//             UID=WF10DLL1,JOBNAME=WF10DLL1                                    
//LOAD.SYSIN DD DSN=&&DLINARC,DISP=(OLD,DELETE,DELETE)                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WF10DLL1                                         
//*                                                                             
