//W330J03D JOB (670W3300100W330J03D,W100),'RTN W330R1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//********************************************************************          
//*         VCOM-ÖVERFÖRING AV ARTIKELINFO MB                        *          
//********************************************************************          
//*                                                                             
//* ---------- STEG 1. SKAPA EN 'DEL-FIL' ATT SKICKA -----------------          
//*                                                                             
//*                                                                             
//W330EMP  EXEC WEMPTST,DSIN=W330.W330R1.W3303D(+0)                             
//*TOMIF    IF W330EMP.T.RC = 0 THEN                                            
//*W330     EXEC W330P03D,                                                      
//*             INDIN=W330.W330R1,                                              
//*             INDUT=W330.W330R1                                               
//*                                                                             
//*                                                                             
//* ----------- STEG 2. SKICKA 'DEL-FIL' VIA VCOM --------------------          
//*                                                                             
//*VCOM     EXEC W016P022,VCOM=W330Z1M4                                         
//*                                                                             
//*                                                                             
//*W01622.W016ZZD1 DD DSN=&&W3303DV,DISP=(OLD,DELETE,DELETE)                    
//************ FIL MED ARTINFO                                                  
//************ 'DEL-FIL' FRÅN W330P03D ATT SÄNDA VIA VCOM                       
//*                                                                             
//*                                                                             
//*                                                                             
//* --- STEG 3. BESTÄLL JOB IGEN OM RETURKOD 8 FRÅN PGM W3303D -------          
//*                                                                             
//*OPBEST EXEC WSOP,COND=(8,NE,W330.W3303D)                                     
//*ORDER W330J03D                                                               
//*                                                                             
//*       ENDIF                                                                 
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W330J03D                                         
