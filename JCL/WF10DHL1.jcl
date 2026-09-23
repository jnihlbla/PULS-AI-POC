//WF10DHL1 JOB (650WF100100WF10DHL1,W100),'RTN WF10D5',                         
//             CLASS=K,                                                         
//             USER=?,PASSWORD=?                                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*CHANGE THE TABLE FROM T01DHEA TO T01DHEA_ARC IN LOAD CARD                    
//SORT1    EXEC PGM=SORT                                                        
//SYSOUT     DD SYSOUT=*                                                        
//SYSPRINT   DD SYSOUT=*                                                        
//SORTIN     DD DSN=WF01.SYSPUNCH.S01DHEA(0),DISP=SHR                           
//SYSIN      DD *                                                               
  OPTION COPY                                                                   
  OUTREC FINDREP=(IN=C'T01DHEA',OUT=C'T01DHEA_ARC')                             
//SORTOUT    DD DSN=&&DHEAARC,                                                  
//             DISP=(NEW,PASS,DELETE),                                          
//             DATACLAS=PSEN                                                    
//*                                                                             
//*LOAD DATA TO T01DHEA_ARC                                                     
//LOAD     EXEC WG02LOAD,DSIN=WF01.DISCARD.S01DHEA(0),                          
//             UID=WF10DHL1,JOBNAME=WF10DHL1                                    
//LOAD.SYSIN DD DSN=&&DHEAARC,DISP=(OLD,DELETE,DELETE)                          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WF10DHL1                                         
//*                                                                             
