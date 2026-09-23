//W118J014 JOB (640W1180100W118J014,W100),'RTN W118D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST1                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W118.W118D1.W11814(0)                                
//EMPTY2 EXEC WEMPTST,DSIN=W118.W118D1.W11814(-1)                               
//*                                                                             
//    IF (EMPTY1.T.RC = 0 OR EMPTY2.T.RC = 0 ) THEN                             
//W118     EXEC W118P014                                                        
//*Error file to TCPLM                                                          
//DAP EXEC WZ14DAP3,DSIN=W118.W118D1.W11814X(+1)                                
//*                                                                             
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W118J014                                         
