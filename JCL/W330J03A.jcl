//W330J03A JOB (670W3300100W330J03A,W100),'RTN W330R1',                         
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
//W330EMP  EXEC WEMPTST,DSIN=W330.W330R1.W3303A(+0)                             
//TOMIF    IF W330EMP.T.RC = 0 THEN                                             
//W330     EXEC W330P03A,                                                       
//             INDIN=W330.W330R1,                                               
//             INDUT=W330.W330R1                                                
//*                                                                             
//*                                                                             
//* ----------- STEG 2. SKICKA 'DEL-FIL' VIA VCOM --------------------          
//*                                                                             
//VCOM     EXEC W016P022,VCOM=W330Z1M1                                          
//*                                                                             
//*                                                                             
//W01622.W016ZZD1 DD DSN=&&W3303AV,DISP=(OLD,DELETE,DELETE)                     
//************ FIL MED ARTINFO                                                  
//************ 'DEL-FIL' FRÅN W330P03A ATT SÄNDA VIA VCOM                       
//*                                                                             
//*                                                                             
//*                                                                             
//* --- STEG 3. BESTÄLL JOB IGEN OM RETURKOD 8 FRÅN PGM W3303A -------          
//*                                                                             
//SOPBEST EXEC WSOP,COND=(8,NE,W330.W3303A)                                     
 ORDER W330J03A                                                                 
//*                                                                             
//        ENDIF                                                                 
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W330J03A                                         
