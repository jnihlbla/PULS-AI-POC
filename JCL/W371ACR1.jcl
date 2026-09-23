//W371ACR1 JOB (650W3710100W371ACR1,W100),'RTN W371V9',                         
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
//             JOBNAME=W371ACR1,UID=W371ACR1                                    
REORG TABLESPACE DW371.SBYLACK                                                  
REORG INDEX      WDB2.XBYLACK1                                                  
REORG INDEX      WDB2.XBYLACK2                                                  
REORG INDEX      WDB2.XBYLACK4                                                  
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W371ACR1                                         
//*                                                                             
