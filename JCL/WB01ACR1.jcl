//WB01ACR1 JOB (650WB010100WB01ACR1,W100),'RTN W012V1',                         
//             CLASS=K,                                                         
//             USER=?,PASSWORD=?                                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//REORG   EXEC PROC=WG02REOR,SYSTEM=D2G0,                                       
//             JOBNAME=WB01ACR1,UID=WB01ACR1                                    
//SYSCOPY     DD       DSN=WG02.DUMP.SB1ACCE(+1),                               
//             DISP=(,CATLG,DELETE),                                            
//             SPACE=(TRK,(900,3000),RLSE),                                     
//             MGMTCLAS=NOBACKUP,DATACLAS=MVOL                                  
REORG TABLESPACE DWB01.SB1ACCE SHRLEVEL CHANGE                                  
      MAPPINGTABLE WDB2.STCHANG                                                 
RUNSTATS TABLESPACE DWB01.SB1ACCE INDEX(ALL) SHRLEVEL CHANGE                    
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WB01ACR1                                         
//*                                                                             
