using System;
using System.Globalization;
using APMaths.Interpreter;
using System.Net.NetworkInformation;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Shapes;

namespace APMaths.WpfApp
{
    public partial class MainWindow : Window
    {
        // origin at graph canvas centre (x to right y up )
        private const double UnitsPerPixel = 1.0 / 40; // 1 unit = 40 pixels
        private const double PixelsPerUnit = 40;

        public MainWindow()
        {
            InitializeComponent();
            SizeChanged += (_, __) => DrawAxes();
            Loaded += (_, __) => DrawAxes();
        }

        // interpreter ui
        private void BtnEval_Click(object sender, RoutedEventArgs e)
        {
            string inputExpression = InputExpr.Text ?? string.Empty;

            // calls into the F# module
            var evaluationResult = APMaths.Interpreter.Interpreter.Evaluate(InputExpr.Text ?? string.Empty);

            switch (evaluationResult)
            {
                case APMaths.Interpreter.Interpreter.EvalResult.Ok success:
                    OutputResult.Text = success.Item.ToString("G", CultureInfo.InvariantCulture);
                    StatusText.Text = "Successful";
                    break;

                case APMaths.Interpreter.Interpreter.EvalResult.Error failure:
                    OutputResult.Text = string.Empty;
                    StatusText.Text = failure.Item;
                    break;
            }
        }

        // plotting ui
        private void BtnDraw_OnClick(object sender, RoutedEventArgs e)
        {
            if (!TryParseInvariant(SlopeInput.Text, out double slopeM)) { StatusText.Text = "Invalid slope"; return; }
            if (!TryParseInvariant(InterceptInput.Text, out double interceptB)) { StatusText.Text = "Invalid intercept"; return; }

            DrawAxes();
            DrawLineAcrossCanvas(slopeM, interceptB);
            StatusText.Text = $"Drawn line: y = {slopeM}·x + {interceptB}";
        }

        private void BtnClear_OnClick(object sender, RoutedEventArgs e)
        {
            PlotCanvas.Children.Clear();
            DrawAxes();
            StatusText.Text = "Cleared";
        }

        private void BtnDrawLine_Click(object sender, RoutedEventArgs e)
        {
            if (!TryParseInvariant(SlopeInput.Text, out double slopeM))
            {
                StatusText.Text = "Invalid slope";
                return;
            }
            if (!TryParseInvariant(InterceptInput.Text, out double interceptB))
            {
                StatusText.Text = "Invalid intercept";
                return;
            }

            // keep lines and axes and just adds a new one
            if (PlotCanvas.Children.Count == 0)
            {
                DrawAxes();
            }
            DrawLineAcrossCanvas(slopeM, interceptB);

            StatusText.Text = $"Drawn line: y = {slopeM}·x + {interceptB}";
        }


        private void HelpToggleMenuItem_OnClick(object sender, RoutedEventArgs e)
        { HelpPane.Visibility = HelpPane.Visibility == Visibility.Visible ? Visibility.Collapsed : Visibility.Visible;}

        private void HelpToggleCheckBox_OnChecked(object sender, RoutedEventArgs e)
        { HelpPane.Visibility = Visibility.Visible;}

        private void HelpToggleCheckBox_OnUnchecked(object sender, RoutedEventArgs e)
        { HelpPane.Visibility = Visibility.Collapsed;}

        // drawing helpers
        private static bool TryParseInvariant(string? text, out double value)
        { return double.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture, out value);}

        private void DrawAxes()
        {
            if (PlotCanvas == null) return;

            double canvasWidth = PlotCanvas.ActualWidth;
            double canvasHeight = PlotCanvas.ActualHeight;
            PlotCanvas.Children.Clear();

            // axes
            double centreX = canvasWidth / 2.0;
            double centreY = canvasHeight / 2.0;
            var gridBrush = new SolidColorBrush(Color.FromRgb(180, 180, 180));
            var axisBrush = new SolidColorBrush(Color.FromRgb(120, 120, 120));

            var xAxis = new Line { X1 = 0, Y1 = centreY, X2 = canvasWidth, Y2 = centreY, Stroke = axisBrush, StrokeThickness = 1.2 };
            var yAxis = new Line { X1 = centreX, Y1 = 0, X2 = centreX, Y2 = canvasHeight, Stroke = axisBrush, StrokeThickness = 1.2 };
            PlotCanvas.Children.Add(xAxis);
            PlotCanvas.Children.Add(yAxis);

            // grid lines
            for (double x = centreX % PixelsPerUnit; x < canvasWidth; x += PixelsPerUnit)
            {
                var verticalGridLine = new Line { X1 = x, Y1 = 0, X2 = x, Y2 = canvasHeight, Stroke = gridBrush, StrokeThickness = 0.5 };
                PlotCanvas.Children.Add(verticalGridLine);
            }
            for (double y = centreY % PixelsPerUnit; y < canvasHeight; y += PixelsPerUnit)
            {
                var horizontalGridLine = new Line { X1 = 0, Y1 = y, X2 = canvasWidth, Y2 = y, Stroke = gridBrush, StrokeThickness = 0.5 };
                PlotCanvas.Children.Add(horizontalGridLine);
            }
        }

        private void DrawLineAcrossCanvas(double slopeM, double interceptB)
        {
            double canvasWidth = PlotCanvas.ActualWidth;
            double canvasHeight = PlotCanvas.ActualHeight;
            double xWorldMin = -(canvasWidth / 2.0) * UnitsPerPixel;
            double xWorldMax = +(canvasWidth / 2.0) * UnitsPerPixel;
            double yWorldAtMin = slopeM * xWorldMin + interceptB;
            double yWorldAtMax = slopeM * xWorldMax + interceptB;

            (double x1, double y1) = ConvertWorldToCanvasCoordinates(xWorldMin, yWorldAtMin);
            (double x2, double y2) = ConvertWorldToCanvasCoordinates(xWorldMax, yWorldAtMax);

            var lineShape = new Line
            {
                X1 = x1,
                Y1 = y1,
                X2 = x2,
                Y2 = y2,
                Stroke = Brushes.SteelBlue,
                StrokeThickness = 2.0
            };
            PlotCanvas.Children.Add(lineShape);
        }

        private (double x, double y) ConvertWorldToCanvasCoordinates(double xWorld, double yWorld)
        {
            double canvasWidth = PlotCanvas.ActualWidth;
            double canvasHeight = PlotCanvas.ActualHeight;
            double xCanvas = canvasWidth / 2.0 + xWorld * PixelsPerUnit;  // right
            double yCanvas = canvasHeight / 2.0 - yWorld * PixelsPerUnit; // up
            return (xCanvas, yCanvas);
        }
    }
    }
