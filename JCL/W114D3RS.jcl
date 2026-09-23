//W114D3RS   JOB (640W1140100W114D3RS,W100),'RTN W114D3',                       
//             CLASS=L,USER=?,PASSWORD=?                                        
/*JOBPARM LINECT=0,FORMS=1800                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//*                                                                             
//WBLOCK EXEC WBLOCK,NAME=W114D3                                                
//*                                                                             
//SPOC      EXEC PGM=CSLUSPOC,                                                  
//  PARM=('IMSPLEX=IMG0,ROUTE=*,WAIT=30')                                       
//SYSPRINT  DD SYSOUT=*                                                         
//SYSIN     DD *                                                                
 UPDATE DB NAME(WDF7*) STOP(ACCESS)                                             
//*                                                                             
//WAIT    EXEC WWAIT,SECONDS=10                                                 
//*                                                                             
//* KOPIERA NATTENS VCOM-FIL                                                    
//*                                                                             
//ICEGENER EXEC PGM=V16459,PARM='ICEGENER/3/'                                   
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  DUMMY                                                            
//SYSUT1   DD  DSN=W114.W114X3SE.W11410(+0),DISP=SHR                            
//************ SENASTE VCOM-FIL MED MPNR-DATA FRÅN FORD                         
//*                                                                             
//SYSUT2   DD  DSN=W114.W114D3.W11410(+1),        -LIM(9)                       
//             DISP=(NEW,CATLG,DELETE),                                         
//************ PRELIMINÄR LADDFIL FÖR DATABAS WDF7                              
//             MGMTCLAS=DEL20BKP,DATACLAS=PSEN                                  
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W114D3RS                                         
