//W335J141 JOB (670W3350100W335J141,W100),'RTN W335B2',                         
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
//W335     EXEC W335P041,                                                       
//             INDIN=W335.W335B2,                                               
//             INDUT=W335.W335B2                                                
//*                                                                             
//*33541.W33541D1 DD DSN=W.QASE.VCOMPARM(W335Z1MX),DISP=SHR                     
//************ CONSTANTMEDLEM MED VCOMINFO                                      
//*                                                                             
//* ----------- STEG 2. SKICKA 'DEL-FIL' VIA VCOM --------------------          
//*                           W335Z1M0 = VCAS                                   
//*                           W335Z1M1 = SVERIGE                                
//*                           W335Z1M2 = VCEM                                   
//*                           W335Z1M3 = ASIA/PACFIC                            
//*                           W335Z1M4 = VCSA SOUTH AMERICA                     
//*                           W335Z1M5 = VCNA                                   
//*                           W335Z1M6 = VCI CIS                                
//VCOM     EXEC W016P022,VCOM=&VCOM                                             
&VCOM                                                                           
//*                                                                             
//*                                                                             
//W01622.W016ZZD1 DD DSN=W335.W335B2.W33540V(+1),DISP=SHR                       
//************ FIL MED ARTINFO                                                  
//************ 'DEL-FIL' FRÅN W335P041 ATT SÄNDA VIA VCOM                       
//*                                                                             
//* --- STEG 3. BESTÄLL JOB IGEN OM RETURKOD 8 FRÅN PGM W33541 -------          
//*                                                                             
//SOPBEST EXEC WSOP,COND=(8,NE,W335.W33541)                                     
 ORDER W335J141                                                                 
//*                                                                             
//*                                                                             
//* --- STEG 4. KATALOGISERA FIL MED EV. POSTER KVAR ATT SÄNDA -------          
//*                                                                             
//CATLG    EXEC PGM=IEFBR14                                                     
//CB20XX01 DD  DSN=W335.W335B2.W33540(+1),                                      
//             DISP=(OLD,CATLG,DELETE)                                          
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J141                                         
