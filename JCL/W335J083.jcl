//W335J083 JOB (640W3350100W335J083,W100),'RTN W335B9',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*                                                                             
//*                                                                             
//********************************************************************          
//*         VCOM-ÖVERFÖRING AV SPRÅKINFO   MB                        *          
//********************************************************************          
//*                                                                             
//* ---------- STEG 1. SKAPA EN 'DEL-FIL' ATT SKICKA -----------------          
//*                                                                             
//*                                                                             
//W335     EXEC W335P083                                                        
//*                                                                             
//* ----------- STEG 2. SKICKA 'DEL-FIL' VIA VCOM --------------------          
//EMPTY1  EXEC WEMPTST,DSIN=W335.W335B9M0.W33581(+0)                            
//*                                                                             
//    IF (EMPTY1.T.RC NE 4) THEN                                                
//*                                                                             
//*                                                                             
//VCOM1    EXEC W016P022,VCOM=W335Z9M0                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W335.W335B9.W33581V(+1),DISP=SHR                       
//************ FIL MED ARTINFO                                                  
//************ 'DEL-FIL' FRÅN W335P083 ATT SÄNDA VIA VCOM                       
//*                                                                             
//*                                                                             
//    ENDIF                                                                     
//*                                                                             
//EMPTY2  EXEC WEMPTST,DSIN=W335.W335B9M1.W33581(+0)                            
//*                                                                             
//    IF (EMPTY2.T.RC NE 4) THEN                                                
//*                                                                             
//*                                                                             
//VCOM2    EXEC W016P022,VCOM=W335Z9M1                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W335.W335B9.W33581V(+1),DISP=SHR                       
//************ FIL MED ARTINFO                                                  
//************ 'DEL-FIL' FRÅN W335P083 ATT SÄNDA VIA VCOM                       
//*                                                                             
//*                                                                             
//    ENDIF                                                                     
//*                                                                             
//EMPTY3  EXEC WEMPTST,DSIN=W335.W335B9M2.W33581(+0)                            
//*                                                                             
//    IF (EMPTY3.T.RC NE 4) THEN                                                
//*                                                                             
//*                                                                             
//VCOM3    EXEC W016P022,VCOM=W335Z9M2                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W335.W335B9.W33581V(+1),DISP=SHR                       
//************ FIL MED ARTINFO                                                  
//************ 'DEL-FIL' FRÅN W335P083 ATT SÄNDA VIA VCOM                       
//*                                                                             
//*                                                                             
//    ENDIF                                                                     
//*                                                                             
//EMPTY4  EXEC WEMPTST,DSIN=W335.W335B9M3.W33581(+0)                            
//*                                                                             
//    IF (EMPTY4.T.RC NE 4) THEN                                                
//*                                                                             
//*                                                                             
//VCOM4    EXEC W016P022,VCOM=W335Z9M3                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W335.W335B9.W33581V(+1),DISP=SHR                       
//************ FIL MED ARTINFO                                                  
//************ 'DEL-FIL' FRÅN W335P083 ATT SÄNDA VIA VCOM                       
//*                                                                             
//*                                                                             
//    ENDIF                                                                     
//*                                                                             
//*EMPTY5  EXEC WEMPTST,DSIN=W335.W335B9M4.W33581(+0)                           
//*                                                                             
//*    IF (EMPTY5.T.RC NE 4) THEN                                               
//*                                                                             
//*                                                                             
//*VCOM5    EXEC W016P022,VCOM=W335Z9M4                                         
//*                                                                             
//*W01622.W016ZZD1 DD DSN=&&W33581V,DISP=(OLD,PASS)                             
//************ FIL MED ARTINFO                                                  
//************ 'DEL-FIL' FRÅN W335P083 ATT SÄNDA VIA VCOM                       
//*                                                                             
//*                                                                             
//*   ENDIF                                                                     
//*                                                                             
//*EMPTY6  EXEC WEMPTST,DSIN=W335.W335B9M6.W33581(+0)                           
//*                                                                             
//*    IF (EMPTY6.T.RC NE 4) THEN                                               
//*                                                                             
//*                                                                             
//*VCOM6    EXEC W016P022,VCOM=W335Z9M6                                         
//*                                                                             
//*W01622.W016ZZD1 DD DSN=&&W33581V,DISP=(OLD,PASS)                             
//************ FIL MED ARTINFO                                                  
//************ 'DEL-FIL' FRÅN W335P083 ATT SÄNDA VIA VCOM                       
//*                                                                             
//*                                                                             
//*   ENDIF                                                                     
//*                                                                             
//*EMPTYT  EXEC WEMPTST,DSIN=W335.W335B9TT.W33581(+0)                           
//*                                                                             
//*    IF (EMPTYT.T.RC NE 4) THEN                                               
//*                                                                             
//*                                                                             
//*VCOMT    EXEC W016P022,VCOM=W335Z9TT                                         
//*                                                                             
//*W01622.W016ZZD1 DD DSN=&&W33581V,DISP=(OLD,PASS)                             
//************ FIL MED ARTINFO                                                  
//************ 'DEL-FIL' FRÅN W335P083 ATT SÄNDA VIA VCOM                       
//*                                                                             
//*                                                                             
//*    ENDIF                                                                    
//*                                                                             
//*                                                                             
//* --- STEG 3. BESTÄLL JOB IGEN OM RETURKOD 8 FRÅN PGM W33583 -------          
//*                                                                             
//SOPBEST EXEC WSOP,COND=(8,NE,W335.W33583)                                     
 ORDER W335J083                                                                 
//*                                                                             
//*                                                                             
//* --- STEG 4. KATALOGISERA FIL MED EV. POSTER KVAR ATT SÄNDA -------          
//*                                                                             
//CATLG    EXEC PGM=IEFBR14                                                     
//CB20XX01 DD  DSN=W335.W335B9.W33581(+1),                                      
//             DISP=(OLD,CATLG,DELETE)                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J083                                         
