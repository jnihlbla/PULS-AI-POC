//W412J09X JOB (640W4120100W412J09X,W100),'RTN W412D9',                         
//             CLASS=L,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*****************************************************************             
//*                                                                             
//* ÖVERFÖRING AV SERVICEKONTRAKT TILL VIPS, EN FIL PER MARKNAD                 
//* ALTERNATIVT BORTTAG AV FIL OM FILEN ÄR TOM                                  
//*                                                                             
//* VI ANVÄNDER W461:S VCOM-PARTNERS FÖR SERVICEKONTRAKTEN                      
//*                                                                             
//* WWDIST36 STYR VILKA DIST SOM VÄLJS UT OCH SKICKAS                           
//*****************************************************************             
//*                                                                             
//EMPTYT1 EXEC WEMPTST,DSIN=W412.W412D9.W41290SE(+0)                            
//*                                                                             
//   IF (EMPTYT1.T.RC = 4) THEN                                                 
//DEL1  EXEC PGM=IEFBR14                                                        
//DD1   DD DSN=W412.W412D9.W41290SE(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM1    EXEC W016P022,VCOM=W461Z1SE                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290SE(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*                                                                             
//EMPTYT2 EXEC WEMPTST,DSIN=W412.W412D9.W41290NO(+0)                            
//*                                                                             
//   IF (EMPTYT2.T.RC = 4) THEN                                                 
//DEL2  EXEC PGM=IEFBR14                                                        
//DD2   DD DSN=W412.W412D9.W41290NO(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM2    EXEC W016P022,VCOM=W461Z1NO                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290NO(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//EMPTYT3 EXEC WEMPTST,DSIN=W412.W412D9.W41290DK(+0)                            
//*                                                                             
//   IF (EMPTYT3.T.RC = 4) THEN                                                 
//DEL3  EXEC PGM=IEFBR14                                                        
//DD3   DD DSN=W412.W412D9.W41290DK(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM3    EXEC W016P022,VCOM=W461Z1DK                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290DK(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT4 EXEC WEMPTST,DSIN=W412.W412D9.W41290FI(+0)                            
//*                                                                             
//   IF (EMPTYT4.T.RC = 4) THEN                                                 
//DEL4  EXEC PGM=IEFBR14                                                        
//DD4   DD DSN=W412.W412D9.W41290FI(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM4    EXEC W016P022,VCOM=W461Z2FI                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290FI(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT5 EXEC WEMPTST,DSIN=W412.W412D9.W41290BE(+0)                            
//*                                                                             
//   IF (EMPTYT5.T.RC = 4) THEN                                                 
//DEL5  EXEC PGM=IEFBR14                                                        
//DD5   DD DSN=W412.W412D9.W41290BE(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM5    EXEC W016P022,VCOM=W461Z1BE                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290BE(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT6 EXEC WEMPTST,DSIN=W412.W412D9.W41290GB(+0)                            
//*                                                                             
//   IF (EMPTYT6.T.RC = 4) THEN                                                 
//DEL6  EXEC PGM=IEFBR14                                                        
//DD6   DD DSN=W412.W412D9.W41290GB(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM6    EXEC W016P022,VCOM=W461Z1GB                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290GB(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//EMPTYT7 EXEC WEMPTST,DSIN=W412.W412D9.W41290FR(+0)                            
//*                                                                             
//   IF (EMPTYT7.T.RC = 4) THEN                                                 
//DEL7  EXEC PGM=IEFBR14                                                        
//DD7   DD DSN=W412.W412D9.W41290FR(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM7    EXEC W016P022,VCOM=W461Z1FR                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290FR(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT8 EXEC WEMPTST,DSIN=W412.W412D9.W41290NL(+0)                            
//*                                                                             
//   IF (EMPTYT8.T.RC = 4) THEN                                                 
//DEL8  EXEC PGM=IEFBR14                                                        
//DD8   DD DSN=W412.W412D9.W41290NL(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM8    EXEC W016P022,VCOM=W461Z1NL                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290NL(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT9 EXEC WEMPTST,DSIN=W412.W412D9.W41290IE(+0)                            
//*                                                                             
//   IF (EMPTYT9.T.RC = 4) THEN                                                 
//DEL9  EXEC PGM=IEFBR14                                                        
//DD9   DD DSN=W412.W412D9.W41290IE(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM9    EXEC W016P022,VCOM=W461Z1IE                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290IE(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT10 EXEC WEMPTST,DSIN=W412.W412D9.W41290IT(+0)                           
//*                                                                             
//   IF (EMPTYT10.T.RC = 4) THEN                                                
//DEL10 EXEC PGM=IEFBR14                                                        
//DD10  DD DSN=W412.W412D9.W41290IT(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM10   EXEC W016P022,VCOM=W461Z1IT                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290IT(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT11 EXEC WEMPTST,DSIN=W412.W412D9.W41290PT(+0)                           
//*                                                                             
//   IF (EMPTYT11.T.RC = 4) THEN                                                
//DEL11 EXEC PGM=IEFBR14                                                        
//DD11  DD DSN=W412.W412D9.W41290PT(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM11   EXEC W016P022,VCOM=W461Z1PT                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290PT(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT12  EXEC WEMPTST,DSIN=W412.W412D9.W41290PX(+0)                          
//*                                                                             
//   IF (EMPTYT12.T.RC = 4) THEN                                                
//DEL12 EXEC PGM=IEFBR14                                                        
//DD12  DD DSN=W412.W412D9.W41290PX(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM12   EXEC W016P022,VCOM=W461Z1PX                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290PX(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT13 EXEC WEMPTST,DSIN=W412.W412D9.W41290CH(+0)                           
//*                                                                             
//   IF (EMPTYT13.T.RC = 4) THEN                                                
//DEL13 EXEC PGM=IEFBR14                                                        
//DD13  DD DSN=W412.W412D9.W41290CH(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM13   EXEC W016P022,VCOM=W461Z1CH                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290CH(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT14  EXEC WEMPTST,DSIN=W412.W412D9.W41290ES(+0)                          
//*                                                                             
//   IF (EMPTYT14.T.RC = 4) THEN                                                
//DEL14 EXEC PGM=IEFBR14                                                        
//DD14  DD DSN=W412.W412D9.W41290ES(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM14   EXEC W016P022,VCOM=W461Z1ES                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290ES(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT15 EXEC WEMPTST,DSIN=W412.W412D9.W41290DE(+0)                           
//*                                                                             
//   IF (EMPTYT15.T.RC = 4) THEN                                                
//DEL15 EXEC PGM=IEFBR14                                                        
//DD15  DD DSN=W412.W412D9.W41290DE(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM15   EXEC W016P022,VCOM=W461Z1DE                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290DE(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT16 EXEC WEMPTST,DSIN=W412.W412D9.W41290AT(+0)                           
//*                                                                             
//   IF (EMPTYT16.T.RC = 4) THEN                                                
//DEL16 EXEC PGM=IEFBR14                                                        
//DD16  DD DSN=W412.W412D9.W41290AT(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM16   EXEC W016P022,VCOM=W461Z1AT                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290AT(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT17 EXEC WEMPTST,DSIN=W412.W412D9.W41290PL(+0)                           
//*                                                                             
//   IF (EMPTYT17.T.RC = 4) THEN                                                
//DEL17 EXEC PGM=IEFBR14                                                        
//DD17  DD DSN=W412.W412D9.W41290PL(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM17   EXEC W016P022,VCOM=W461Z1PL                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290PL(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT18 EXEC WEMPTST,DSIN=W412.W412D9.W41290SA(+0)                           
//*                                                                             
//   IF (EMPTYT18.T.RC = 4) THEN                                                
//DEL18 EXEC PGM=IEFBR14                                                        
//DD18  DD DSN=W412.W412D9.W41290SA(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM18   EXEC W016P022,VCOM=W461Z1SA                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290SA(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT19 EXEC WEMPTST,DSIN=W412.W412D9.W41290MY(+0)                           
//*                                                                             
//   IF (EMPTYT19.T.RC = 4) THEN                                                
//DEL19 EXEC PGM=IEFBR14                                                        
//DD19  DD DSN=W412.W412D9.W41290MY(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM19   EXEC W016P022,VCOM=W461Z1MY                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290MY(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT20 EXEC WEMPTST,DSIN=W412.W412D9.W41290TR(+0)                           
//*                                                                             
//   IF (EMPTYT20.T.RC = 4) THEN                                                
//DEL20 EXEC PGM=IEFBR14                                                        
//DD20  DD DSN=W412.W412D9.W41290TR(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM20   EXEC W016P022,VCOM=W461Z1TR                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290TR(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT21 EXEC WEMPTST,DSIN=W412.W412D9.W41290KR(+0)                           
//*                                                                             
//   IF (EMPTYT21.T.RC = 4) THEN                                                
//DEL21 EXEC PGM=IEFBR14                                                        
//DD21  DD DSN=W412.W412D9.W41290KR(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM21   EXEC W016P022,VCOM=W461Z1KR                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290KR(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT22 EXEC WEMPTST,DSIN=W412.W412D9.W41290TX(+0)                           
//*                                                                             
//   IF (EMPTYT22.T.RC = 4) THEN                                                
//DEL22 EXEC PGM=IEFBR14                                                        
//DD22  DD DSN=W412.W412D9.W41290TX(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM22   EXEC W016P022,VCOM=W461Z1TX                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290TX(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT23 EXEC WEMPTST,DSIN=W412.W412D9.W41290TW(+0)                           
//*                                                                             
//   IF (EMPTYT23.T.RC = 4) THEN                                                
//DEL23 EXEC PGM=IEFBR14                                                        
//DD23  DD DSN=W412.W412D9.W41290TW(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM23   EXEC W016P022,VCOM=W461Z1TW                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290TW(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT24 EXEC WEMPTST,DSIN=W412.W412D9.W41290MX(+0)                           
//*                                                                             
//   IF (EMPTYT24.T.RC = 4) THEN                                                
//DEL24 EXEC PGM=IEFBR14                                                        
//DD24  DD DSN=W412.W412D9.W41290MX(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM24   EXEC W016P022,VCOM=W461Z1MX                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290MX(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT25 EXEC WEMPTST,DSIN=W412.W412D9.W41290BR(+0)                           
//*                                                                             
//   IF (EMPTYT25.T.RC = 4) THEN                                                
//DEL25 EXEC PGM=IEFBR14                                                        
//DD25  DD DSN=W412.W412D9.W41290BR(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM25   EXEC W016P022,VCOM=W461Z1BR                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290BR(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT26 EXEC WEMPTST,DSIN=W412.W412D9.W41290CA(+0)                           
//*                                                                             
//   IF (EMPTYT26.T.RC = 4) THEN                                                
//DEL26 EXEC PGM=IEFBR14                                                        
//DD26  DD DSN=W412.W412D9.W41290CA(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM26   EXEC W016P022,VCOM=W461Z1CA                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290CA(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT27 EXEC WEMPTST,DSIN=W412.W412D9.W41290JP(+0)                           
//*                                                                             
//   IF (EMPTYT27.T.RC = 4) THEN                                                
//DEL27 EXEC PGM=IEFBR14                                                        
//DD27  DD DSN=W412.W412D9.W41290JP(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM27   EXEC W016P022,VCOM=W461Z1JP                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290JP(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT29 EXEC WEMPTST,DSIN=W412.W412D9.W41290C1(+0)                           
//*                                                                             
//   IF (EMPTYT29.T.RC = 4) THEN                                                
//DEL29 EXEC PGM=IEFBR14                                                        
//DD29  DD DSN=W412.W412D9.W41290C1(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM29   EXEC W016P022,VCOM=W461Z1C1                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290C1(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT30  EXEC WEMPTST,DSIN=W412.W412D9.W41290PE(+0)                          
//*                                                                             
//   IF (EMPTYT30.T.RC = 4) THEN                                                
//DEL30 EXEC PGM=IEFBR14                                                        
//DD30  DD DSN=W412.W412D9.W41290PE(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM30   EXEC W016P022,VCOM=W461Z1PE                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290PE(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT31 EXEC WEMPTST,DSIN=W412.W412D9.W41290RU(+0)                           
//*                                                                             
//   IF (EMPTYT31.T.RC = 4) THEN                                                
//DEL31 EXEC PGM=IEFBR14                                                        
//DD31  DD DSN=W412.W412D9.W41290RU(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM31   EXEC W016P022,VCOM=W461Z1RU                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290RU(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT32 EXEC WEMPTST,DSIN=W412.W412D9.W41290ZA(+0)                           
//*                                                                             
//   IF (EMPTYT32.T.RC = 4) THEN                                                
//DEL32 EXEC PGM=IEFBR14                                                        
//DD32  DD DSN=W412.W412D9.W41290ZA(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM32   EXEC W016P022,VCOM=W461Z1ZA                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290ZA(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT33 EXEC WEMPTST,DSIN=W412.W412D9.W41290US(+0)                           
//*                                                                             
//   IF (EMPTYT33.T.RC = 4) THEN                                                
//DEL33 EXEC PGM=IEFBR14                                                        
//DD33  DD DSN=W412.W412D9.W41290US(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM33   EXEC W016P022,VCOM=W461Z1US                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290US(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT34 EXEC WEMPTST,DSIN=W412.W412D9.W41290CZ(+0)                           
//*                                                                             
//   IF (EMPTYT34.T.RC = 4) THEN                                                
//DEL34 EXEC PGM=IEFBR14                                                        
//DD34  DD DSN=W412.W412D9.W41290CZ(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM34   EXEC W016P022,VCOM=W461Z1CZ                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290CZ(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*************                                                                 
//*                                                                             
//EMPTYT35 EXEC WEMPTST,DSIN=W412.W412D9.W41290HU(+0)                           
//*                                                                             
//   IF (EMPTYT35.T.RC = 4) THEN                                                
//DEL35 EXEC PGM=IEFBR14                                                        
//DD35  DD DSN=W412.W412D9.W41290HU(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM35   EXEC W016P022,VCOM=W461Z1HU                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290HU(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*                                                                             
//*************                                                                 
//*                                                                             
//EMPTYT36 EXEC WEMPTST,DSIN=W412.W412D9.W41290IN(+0)                           
//*                                                                             
//   IF (EMPTYT36.T.RC = 4) THEN                                                
//DEL36 EXEC PGM=IEFBR14                                                        
//DD36  DD DSN=W412.W412D9.W41290IN(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM36   EXEC W016P022,VCOM=W461Z1IN                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290IN(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*                                                                             
//*************                                                                 
//*                                                                             
//EMPTYT37 EXEC WEMPTST,DSIN=W412.W412D9.W41290TH(+0)                           
//*                                                                             
//   IF (EMPTYT37.T.RC = 4) THEN                                                
//DEL37 EXEC PGM=IEFBR14                                                        
//DD37  DD DSN=W412.W412D9.W41290TH(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM37   EXEC W016P022,VCOM=W461Z1TH                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290TH(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*                                                                             
//*************                                                                 
//*                                                                             
//EMPTYT38 EXEC WEMPTST,DSIN=W412.W412D9.W41290AU(+0)                           
//*                                                                             
//   IF (EMPTYT38.T.RC = 4) THEN                                                
//DEL38 EXEC PGM=IEFBR14                                                        
//DD38  DD DSN=W412.W412D9.W41290AU(+0),DISP=(OLD,DELETE)                       
//   ELSE                                                                       
//VCOM38   EXEC W016P022,VCOM=W461Z1AU                                          
//*                                                                             
//W01622.W016ZZD1 DD DSN=W412.W412D9.W41290AU(+0),DISP=SHR                      
//*                                                                             
//   ENDIF                                                                      
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W412J09X                                         
