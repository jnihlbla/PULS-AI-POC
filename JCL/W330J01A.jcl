//W330J01A JOB (670W3300100W330J01A,W100),'RTN W330D5',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//********************************************************************          
//*         VCOM-ÖVERFÖRING AV SALES AND TARGET                      *          
//********************************************************************          
//*                                                                             
//* ---------- STEG 1. SKAPA EN FIL ATT SKICKA -----------------                
//*                                                                             
//*                                                                             
//EMPTYT1 EXEC WEMPTST,DSIN=WUT.W330D5.W33018(+0)                               
//*                                                                             
//    IF (EMPTYT1.T.RC = 0) THEN                                                
//*                                                                             
//* SALES AND TARGET ARTIKELINFO DAGLIGEN                                       
//*************  AS-400 SALES AND TARGET                                        
//VCOM     EXEC W016P022,VCOM=W330Z2P0                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=WUT.W330D5.W33018(+0),DISP=SHR                         
//*                                                                             
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W330J01A                                         
