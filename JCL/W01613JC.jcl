//W01613JC JOB (540W0010300W016J013,W100),'RTN W01613',                         
//             CLASS=K                                                          
/*JOBPARM LINES=9,CARDS=0,FORMS=1800                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*                                                                             
//********************************************************************          
//*   SYSTEM    VCOM RECEIVING FILES.                                *          
//*   FUNKTION  THIS JCL WILL BE USED OF PGM R01613 AND DECIDES      *          
//*             WHAT TO DO UPON THE RETURNCODE FROM PGM R01613       *          
//*               1. CREATES JCL TO SUBMIT WHEN FILE ARRIVE.         *          
//*               2. CREATES MEMO AND JCL TO SUBMIT WHEN FILE ARRIVE.*          
//*               3. CREATES MEMO WHEN FILE ARRIVE.                  *          
//*               4. INCORRECT SENDERTAG (NOT A DATASETNAME)         *          
//*                    IN THIS CASE A DATASET ARE CREATED:           *          
//*                        TEST  WIN.TEST.VCOMRCV                    *          
//*                        PROD  WIN.QASE.VCOMRCV                    *          
//*                        AND MEMO WILL BE SENT ACCORDING TO SPEC   *          
//*                        IN SYSIN OF EXPEDITERPROGRAM              *          
//********************************************************************          
