//W488S1RS  JOB (540W4880100W488S1RS,W100),'RTN W488S1',                        
//          CLASS=K,USER=?,PASSWORD=?                                           
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM TIME=1,LINES=5,CARDS=0,FORMS=1800                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//RENAME  EXEC W001PTSO                                                         
//SYSTSIN  DD  *                                                                
 %WRTNIN2 WIN.PDP.W48819  W488.W488S1.W48819                                    
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W488S1RS                                         
