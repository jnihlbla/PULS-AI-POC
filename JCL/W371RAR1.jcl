//W371RAR1 JOB (650W3710100W371RAD1,W100),'RTN W371V9',                         
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
//             JOBNAME=W371RAR1,UID=W371RAR1                                    
REORG TABLESPACE DW371.SBYLRAD                                                  
REORG INDEX      WDB2.XBYLRAD1                                                  
REORG INDEX      WDB2.XBYLRAD3                                                  
REORG INDEX      WDB2.XBYLRAD4                                                  
REORG INDEX      WDB2.XBYLRAD5                                                  
REORG INDEX      WDB2.XBYLRAD6                                                  
REORG INDEX      WDB2.XBYLRAD7                                                  
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W371RAR1                                         
//*                                                                             
