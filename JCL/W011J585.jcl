//W011J585 JOB (640W0110100W011J585,W100),'RTN W011D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//SORT    EXEC PGM=SORT                                                         
//SYSOUT   DD  SYSOUT=*                                                         
//*                                                                             
//SORTIN   DD  DSN=W011.LDC.W01184(+0),DISP=SHR                                 
//         DD  DSN=W011.NDC.W01184(+0),DISP=SHR                                 
//*                                                                             
//SORTOUT  DD  DSN=W011.W011D5.W01184LN(+1),DISP=(NEW,CATLG),                   
//             DATACLAS=PSEN,MGMTCLAS=DEL2BKPC,                                 
//             RECFM=FB,LRECL=48                                                
//*            -- LRECL OVAN MÅSTE STÄMMA MED SUMMA LÄNGD I OUTREC              
//*                                                                             
//SYSIN    DD  *                                                                
 INCLUDE COND=(6,1,CH,EQ,C'7',AND,151,4,PD,GT,0) -- DC7X OCH SALDO>0            
 SORT FIELDS=(1,5,PD,A)          -- SORTERA PÅ ARTIKELNR                        
 SUM  FIELDS=(151,4,PD)          -- SUMMERA SALDONA                             
 OUTFIL OUTREC=(C'CNA',          -- "CNA" I POS 1-3                             
  1,5,PD,ZD,LENGTH=8,            -- ARTNR PACKAT TILL DISPLAY 8 POS             
  151,4,PD,ZD,                   -- SALDO PACKAT TILL DISPLAY (7 POS)           
  C'                              ') -- FILLER 30 POS SPACE                     
//*                                                                             
//VCOM    EXEC W016P022,VCOM=W011Z1C1                                           
//*                                                                             
//W01622.W016ZZD1 DD DSN=W011.W011D5.W01184LN(+1),DISP=SHR                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W011J585                                         
